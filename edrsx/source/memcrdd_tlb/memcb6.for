      subroutine memcb6(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Set up pointers to start of internal files. These files
*	are actually sections of the array ME_st, declared in
*	module ME_COM, and held in common block /MECOMS/.
*
*SOURCE
*       MEMCB6.FOR in MEMCRDD.TLB
*
*METHOD
*	MEMSYS3 uses internal file numbers 1,2 and 20 to 28. Files
*	1 to 20 hold images, files 21 to 40 hold data sets. In addition
*	to these files, MEMCRDD uses ME_st to store:
*
*	   a) 4 images used as work space, 
*	   b) images to hold the FFT of each groups PSF
*	   c) 4 data sets holding SDC identifiers, sample centre
*             coordinates within the output image, and solid angles
*	   d) An image to hold the background image subtracted from 
*             the data before deconvolution.
*
*	Overlaying of files is not used and no data is stored externally
*	on disk.
* 
*	On entry to this routine array ME_st holds data ( written by
*	routine MEMCB5), amounting to 7 data sets. This data is to be
*	reordered and copied to another part of the array. This requires
*	the files to which they are written not to overlap the original
*	files.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A8_COM/,/ME_COM/,/ZZ_COM/
*   WRITE:
*	/B6_COM/,
*		B6_sdc		Pointer to data set holding SDC identifiers
*		B6_x		Pointer to data set holding pixel no. 
*				of sample centre
*		B6_y		Pointer to data set holding line no. of 
*				sample centre
*		B6_wk1		Pointer to start of work image 1
*		B6_wk2		Pointer to start of work image 2
*		B6_wk3		Pointer to start of work image 3
*		B6_wk4		Pointer to start of work image 4
*		B6_psf		Pointer to start of PSF FFTs
*		B6_sol		Pointer to data set holding solid angles
*		B6_bac		Pointer to start of background image
*	/ME_COM/,
*		ME_kb		Pointers to start of files used by MEMSYS3
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*       EDRS:
*              lbgone,wrerr
*              
*STARLINK PARAMETERS
*	B6ERR1(error) 	Accessed if ME_st array is not big enough
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/1/90
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
* INCLUDE COMMON BLOCKS HOLDING...

* ... INFORMATION ABOUT SCAN AND DETECTOR GROUPS
      include '(A8_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      integer	group	! Sample group number
      integer	istat	! Temporary status value
      integer	need	! Size of array needed to store all data OK
      integer	next	! Next available location
      character	prbuf*80! Buffer for screen output

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF THERE IS INSUFFICIENT ROOM IN THE ARRAY TO HOLD ALL THE DATA, TELL
* USER HOW MUCH IS NEEDED AND QUIT
*
      need = 12*ME_mk + (8+A8_ngp)*ME_mj

      if(need.gt.PR_mem) then

         call wrerr('B6ERR1')

         write(prbuf,10) PR_mem,need
  10     format('  Current size is ',I20,' You need ',I20)
         call lbgone(prbuf(49:))
         call lbgone(prbuf(19:))
         call wruser(prbuf,istat)

         ierr=1
         goto 999

*
* IF THERE IS SUFFICIENT ROOM, TELL USER HOW MUCH MEMORY HAS BEEN USED
*
      else

         if(ZZ_ilv.ge.2) then
            call wruser(' ',istat)
            write(prbuf,20) 100.0*real(need)/real(PR_mem)
  20        format('  ',F5.1,'% of internal memory used')
            call lbgone(prbuf(3:))
            call wruser(prbuf,istat)
         endif

      endif

*
* SET UP THE DESTINATION FILES. THESE ARE AT THE HIGH END TO AVOID 
* OVERWRITING THE DATA AT THE LOW END
*
      B6_sdc = PR_mem - ME_mk + 1
      B6_x = B6_sdc - ME_mk
      B6_y = B6_x - ME_mk      
      B6_sol = B6_y - ME_mk
      ME_kb(21) = B6_sol - ME_mk
      ME_kb(22) = ME_kb(21) - ME_mk

*
* AT THE MOMENT NO FILE EQUIVALENCING IS USED, AND ALL WORKSPACE AREAS 
* USED BY OPUS AND TROPUS ARE ALSO STORED IN THE ME_ST ARRAY.
* SET UP INDEX POINTERS TO START OF FILES 1, 2 and 20 WHICH EACH HOLD 
* AN IMAGE USED BY MEMSYS3
*
      ME_kb(1)=1
      ME_kb(2)=ME_kb(1)+ME_mj
      ME_kb(20)=ME_kb(2)+ME_mj

      next=ME_kb(20)+ME_mj
         
*
* SET UP INDEX POINTERS TO START OF FILES 23 TO 28 WHICH EACH HOLD A 
* DATA SET USED BY MEMSYS3 (FILES 21 AND 22 HAVE ALREADY BEEN FIXED).
* 
      ME_kb(23)=next
      ME_kb(24)=ME_kb(23)+ME_mk
      ME_kb(25)=ME_kb(24)+ME_mk
      ME_kb(26)=ME_kb(25)+ME_mk
      ME_kb(27)=ME_kb(26)+ME_mk
      ME_kb(28)=ME_kb(27)+ME_mk

      next=ME_kb(28)+ME_mk

*
* SET UP POINTERS TO THE START OF 4 IMAGES USED FOR WORK SPACE
*
      B6_wk1=next
      B6_wk2=B6_wk1+ME_mj
      B6_wk3=B6_wk2+ME_mj
      B6_wk4=B6_wk3+ME_mj

      next=B6_wk4+ME_mj

*
* SET UP POINTER TO START OF BACKGROUND IMAGE
*
      B6_bac=next
      next=next+ME_mj

*
* SET UP POINTERS TO THE START OF SUFFICIENT IMAGES TO HOLD THE
* FFT OF EACH SAMPLE GROUPS PSF
*
      do group=1,A8_ngp
         B6_psf(group)=next
         next=next+ME_mj
      enddo

*
* FINISH
*
  999 continue

      end
