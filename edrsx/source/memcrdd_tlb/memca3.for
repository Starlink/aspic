      subroutine memca3(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Output an analysis/continuation file to disk in BDF format
*	for use in a later run of MEMCRDD.
*
*SOURCE
*       MEMCA3.FOR in MEMCRDD.TLB
*
*METHOD
*	A pointer to an output BDF is aquired. Each common block (except
*	/MECOMS/) is then copied to the output file as a 1D list of 
*	longword values (this list overlays the actual structure of the
*	common block). The contents of each internal file held in 
*	/MECOMS/ is then copied to the output file.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	All of them
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2diw,updata
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcd6
*       EDRS:
*              wrerr
*
*STARLINK PARAMETERS
*	ANALOUT		The output BDF containing the continuation info
*	A3ERR1(error)	Accessed if an error occured copying data
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
      integer	ipout	! Pointer to output BDF
      integer	istat	! Temporary status value
      integer	size	! No. of longwords required to store all data
      integer	start	! The location within the output BDF at which
			! the first element of the current data set is
			! to be put (updated by MEMCD6).

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* CALCULATE SIZE OF THE DISK FILE REQUIRED TO HOLD CONTENTS OF ALL 
* COMMON BLOCKS (EXCEPT /MECOMS/) AND THE CONTENTS OF ALL USED INTERNAL
* FILES (HELD IN /MECOMS/)
*
      size=A5_sz+ 
     :     A6_sz+ 
     :     A7_sz+
     :     A8_sz+ 
     :     A9_sz+
     :     B0_sz+ 
     :     B1_sz+
     :     B2_sz+ 
     :     B5_sz+ 
     :     B6_sz+ 
     :     D3_sz+ 
     :     D5_sz+ 
     :     E0_sz+
     :     ME_sza+ 
     :     ME_szp+ 
     :     ME_szl+ 
     :     ZZ_sz+ 
     :	   12*ME_mk+(8+A8_ngp)*ME_mj

*
* GET A POINTER TO THE OUTPUT BDF
*
      call gt2diw('ANALOUT',104,.true.,size,1,ipout,istat)
      if(istat.eq.0) then

*
* COPY EACH COMMON BLOCK TO THE OUTPUT BDF
*
         start=1

         call memcd6(A5_cm,A5_sz,%val(ipout),size,start,ierr)
         call memcd6(A6_cm,A6_sz,%val(ipout),size,start,ierr)
         call memcd6(A7_cm,A7_sz,%val(ipout),size,start,ierr)
         call memcd6(A8_cm,A8_sz,%val(ipout),size,start,ierr)
         call memcd6(A9_cm,A9_sz,%val(ipout),size,start,ierr)
         call memcd6(B0_cm,B0_sz,%val(ipout),size,start,ierr)
         call memcd6(B1_cm,B1_sz,%val(ipout),size,start,ierr)
         call memcd6(B2_cm,B2_sz,%val(ipout),size,start,ierr)
         call memcd6(B5_cm,B5_sz,%val(ipout),size,start,ierr)
         call memcd6(B6_cm,B6_sz,%val(ipout),size,start,ierr)
         call memcd6(D3_cm,D3_sz,%val(ipout),size,start,ierr)
         call memcd6(D5_cm,D5_sz,%val(ipout),size,start,ierr)
         call memcd6(E0_cm,E0_sz,%val(ipout),size,start,ierr)
         call memcd6(ME_cma,ME_sza,%val(ipout),size,start,ierr)
         call memcd6(ME_cmp,ME_szp,%val(ipout),size,start,ierr)
         call memcd6(ME_cml,ME_szl,%val(ipout),size,start,ierr)
         call memcd6(ZZ_cm,ZZ_sz,%val(ipout),size,start,ierr)

*
* COPY THE DATA SETS STORED IN /MECOMS/ TO THE BDF
*
         call memcd6(ME_st(ME_kb(21)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(22)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(23)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(24)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(25)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(26)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(27)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(28)),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_sdc),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_x),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_y),ME_mk,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_sol),ME_mk,%val(ipout),size,start,ierr)

*
* COPY THE IMAGES STORED IN /MECOMS/ TO THE BDF
*
         call memcd6(ME_st(ME_kb(1)),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(2)),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(ME_kb(20)),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_wk1),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_wk2),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_wk3),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_wk4),ME_mj,%val(ipout),size,start,ierr)
         call memcd6(ME_st(B6_bac),ME_mj,%val(ipout),size,start,ierr)
         do group=1,A8_ngp
            call memcd6(ME_st(B6_psf(group)),ME_mj,%val(ipout),size,
     :                  start,ierr)
         enddo

*
* CHECK ERROR STATUS
*
         if(ierr.ne.0) call wrerr('A3ERR1')

      endif

*
* CLOSE THE OUTPUT FILE, BUT DO NOT CANCEL THE PARAMETER ASSOCIATION
* IN CASE THE FILE IS TO BE OVERWRITTEN BY A LATER ITERATION.
*
  999 call updata('ANALOUT',istat)

      end
