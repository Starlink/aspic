*      module ME_COM
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Declares common block /MECOMS/, /MECOMP/ and /MECOML/ 
*	which hold data used by MEMSYS3. Also declares common
*	block /MEARGS/ holding MEM3 arguments alpha and beta.
*
*SOURCE
*       ME_COM.INC in MEMCRDD.TLB
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
*-------------------------------------------------------------------
*
* DECLARE MEMSYS3 PARAMETERS
*
      integer	 ME_siz
      parameter  (ME_siz=8)

*
* DECLARE PARAMETERS HOLDING SIZE OF COMMON BLOCKS (IN UNITS OF 4 BYTES)
* AND ALSO INTEGERS WHICH WILL OVERLAY THE START OF EACH COMMON BLOCKS
*
      integer	ME_cma,ME_sza,ME_cms,ME_szs,ME_cmp,ME_szp,ME_cml,ME_szl
      parameter (ME_sza=3,
     :           ME_szs=PR_mem,
     :           ME_szp=172,
     :           ME_szl=1+6*ME_siz)

*
* DECLARE VARIABLES HELD IN COMMON BLOCK /MECOMS/
*
      real	  ME_st(PR_mem) ! Main working space for MEM package

*
* DECLARE AN INTEGER ARRAY TO OVERLAY THE ABOVE REAL ARRAY. THE FIRST 
* SECTION OF THIS WILL HOLD THE DETECTOR NO, CRDD FILE NO., AND SAMPLE
* NO. OF EACH USABLE DATA SAMPLE
*
      integer	  ME_sti(PR_mem)
      equivalence (ME_st,ME_sti)

*
* DECLARE VARIABLES HELD IN COMMON BLOCK /MECOMP/
*
      integer	  ME_nj     ! No. of blocks per image 
      integer	  ME_mj     ! No. of reals in each image block
      integer	  ME_nk     ! No. of blocks per data set 
      integer	  ME_mk     ! No. of reals in each data block
      integer	  ME_ka(40) ! Contain 0's if MEM files are stored 
			    ! internally, otherwise they hold the file
		            ! numbers 1-40
      integer	  ME_kb(40) ! Pointer to start of each "file" held
                            ! in the array ME_st. Initially all set
                            ! to 999999.
      integer     ME_kc(40) ! Used only by MEM package
      integer	  ME_kd(40) ! If ME_ka are not zero, ME_kd holds the 
			    ! pointers to the next "block" of data to
			    ! be accessed by UGET or UPUT
      integer 	  ME_out    ! Fortran unit no. on which to display
                            ! diagnostic information
      integer	  ME_l0     ! Used only by MEM package
      integer	  ME_l1     !           ditto
      integer	  ME_m0     !           ditto
      integer	  ME_m1     !           ditto
      integer	  ME_m2     !           ditto
      integer	  ME_m3     !           ditto
      integer	  ME_ntr    ! No. of calls to OPUS and TROPUS

*
* DECLARE VALUES STORED IN /MECOML/
*
      real*8      ME_xtb(ME_siz)	! Log (alpha) values
      real*8      ME_ytb(ME_siz)	! Log (omega) values
      real*8	  ME_vtb(ME_siz)	! Intrinsic variance values
      integer     ME_ntb		! Current length of table

*
* DECLARE VALUES STORED IN /MEARGS/
*
      real	ME_alp	! Regularisation constant used in MEMSYS3
      real	ME_bet	! Modified alpha obeying distance constraint
      real	ME_sig	! Scale factor which MEMSYS3 applies to given
			! noise values

*
* DEFINE COMMON BLOCKS
*
      common      /MECOMS/  ME_st
      equivalence (ME_st,ME_cms)


      common      /MECOMP/  ME_nj,ME_mj,ME_nk,ME_mk,ME_ka,ME_kb,ME_kc,
     :                      ME_kd,ME_out,ME_l0,ME_l1,ME_m0,ME_m1,ME_m2,
     :                      ME_m3,ME_ntr
      equivalence (ME_nj,ME_cmp)


      common      /MECOML/  ME_xtb,ME_ytb,ME_vtb,ME_ntb
      equivalence (ME_xtb,ME_cml)


      common	  /MEARGS/ ME_alp,ME_bet,ME_sig
      equivalence (ME_alp,ME_cma)

*
* DO ALL REQUIRED INITIALISE 
*
      data        ME_nj     /1/,
     :            ME_nk     /1/,
     :            ME_ka     /40*0/,
     :            ME_kb     /40*999999/
