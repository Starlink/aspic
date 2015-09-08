      subroutine memcd8(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Gets an analysis/continuation file in BDF format from the user
*	and reads in the data it contains to common.
*
*SOURCE
*       MEMCD8.FOR in MEMCRDD.TLB
*
*METHOD
*	A pointer to an input BDF is aquired. Data is then copied
*	from the BDF to each common block (except /MECOMS/) as a 1D list
*	of longword values (this list overlays the actual structure of 
*	the common block). The contents of each internal file held in 
*	/MECOMS/ is then copied from the input file.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   WRITE:
*	All of them
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcd7
*       EDRS:
*              wrerr
*       INTERIM:
*              frdata,cnpar
*
*STARLINK PARAMETERS
*	ANALIN		The input BDF containing the continuation info
*	D8ERR1(error)	Accessed if the common data could not be read
*
*VAX SPECIFICS
*       implicit none
*       %val
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 31/10/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE ALL COMMON BLOCKS
*
      include '(A5_COM)'
      include '(A6_COM)'
      include '(A7_COM)'
      include '(A8_COM)'
      include '(A9_COM)'
      include '(B0_COM)'
      include '(B1_COM)'
      include '(B2_COM)'
      include '(B5_COM)'
      include '(B6_COM)'
      include '(D3_COM)'
      include '(D5_COM)'
      include '(E0_COM)'
      include '(ME_COM)'
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	group	! Sample group no. (there is one PSF for each)
      integer	ipin	! Pointer to input BDF
      integer	istat	! Temporary status value
      integer	nlin	! No. of lines in the given BDF (should be 1)
      integer	size	! Size of input BDF
      integer	start	! The location within the input BDF from which
			! the first element of the current data set is
			! to be read (updated by MEMCD7).

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* GET A POINTER TO THE OUTPUT BDF
*
      call gt2dir('ANALIN',104,.false.,size,nlin,ipin,ierr)

*
* CHECK FILE CONTAINS ONLY 1 LINE
*
      if(nlin.ne.1) ierr=1
      if(ierr.ne.0) goto 999

*
* COPY EACH COMMON BLOCK FROM THE INPUT BDF
*
      start=1

      call memcd7(A5_cm,A5_sz,%val(ipin),size,start,ierr)
      call memcd7(A6_cm,A6_sz,%val(ipin),size,start,ierr)
      call memcd7(A7_cm,A7_sz,%val(ipin),size,start,ierr)
      call memcd7(A8_cm,A8_sz,%val(ipin),size,start,ierr)
      call memcd7(A9_cm,A9_sz,%val(ipin),size,start,ierr)
      call memcd7(B0_cm,B0_sz,%val(ipin),size,start,ierr)
      call memcd7(B1_cm,B1_sz,%val(ipin),size,start,ierr)
      call memcd7(B2_cm,B2_sz,%val(ipin),size,start,ierr)
      call memcd7(B5_cm,B5_sz,%val(ipin),size,start,ierr)
      call memcd7(B6_cm,B6_sz,%val(ipin),size,start,ierr)
      call memcd7(D3_cm,D3_sz,%val(ipin),size,start,ierr)
      call memcd7(D5_cm,D5_sz,%val(ipin),size,start,ierr)
      call memcd7(E0_cm,E0_sz,%val(ipin),size,start,ierr)
      call memcd7(ME_cma,ME_sza,%val(ipin),size,start,ierr)
      call memcd7(ME_cmp,ME_szp,%val(ipin),size,start,ierr)
      call memcd7(ME_cml,ME_szl,%val(ipin),size,start,ierr)
      call memcd7(ZZ_cm,ZZ_sz,%val(ipin),size,start,ierr)

      if(ierr.ne.0) goto 998

*
* COPY THE DATA SETS STORED IN /MECOMS/ FROM THE BDF
*
      call memcd7(ME_st(ME_kb(21)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(22)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(23)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(24)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(25)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(26)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(27)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(28)),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_sdc),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_x),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_y),ME_mk,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_sol),ME_mk,%val(ipin),size,start,ierr)

*
* COPY THE IMAGES STORED IN /MECOMS/ FROM THE BDF
*
      call memcd7(ME_st(ME_kb(1)),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(2)),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(ME_kb(20)),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_wk1),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_wk2),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_wk3),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_wk4),ME_mj,%val(ipin),size,start,ierr)
      call memcd7(ME_st(B6_bac),ME_mj,%val(ipin),size,start,ierr)
      do group=1,A8_ngp
         call memcd7(ME_st(B6_psf(group)),ME_mj,%val(ipin),size,
     :                  start,ierr)
      enddo

*
* CHECK ERROR STATUS
*
  998 if(ierr.ne.0) call wrerr('D8ERR1')

*
* CLOSE THE INPUT, CANCEL THE PARAMETER ASSOCIATION AND FINISH
*
  999 call frdata('ANALIN',istat)
      call cnpar('ANALIN',istat)

      end
