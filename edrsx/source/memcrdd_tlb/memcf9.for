      subroutine memcf9(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Creates a new background image for use in the next run of 
*	MEMCRDD.
*
*SOURCE
*       MEMCF9.FOR in MEMCRDD.TLB
*
*METHOD
*	Data samples which have been "clamped" by the background used
*	to produce the current reconstruction, are identified by the
*	following:
*	a) large negative residuals (the simulated data couldn't
*	   reach the real data because of the positivity constraint
*	   imposed by MEMSYS3).
*	b) Simulated data values close to zero (the simulated data
*	   gets clamped at zero, although it would have liked to go
*	   further if possible).
*	A measure of how much each data sample was "clamped" is 
*	produced. The residuals of those samples which were heavily 
*	clamped are made into an image using TROPUS, and normalised to
*	the coverage image. This image is added onto the current 
*	background, to produce the new background.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*	(The coverage image is assumed to be in file 2)
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*	(On exit ,file 20 holds the new background)
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*   WRITE:
*	/ME_COM/ New background to file 20
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,gt2diw
*       THIS PACKAGE (MEMCRDD.TLB):
*              opus,tropus,memca2
*              
*STARLINK PARAMETERS
*	BACKOUT		The BDF frame to receive the new background
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 9/4/90
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

* ... OUTPUT IMAGE FITS DESCRIPTORS
      include '(B0_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFO
      include '(ME_COM)'

* ... USER PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	c	! Correction factor to apply to the stored 
			! accuracies (calculated by MEMSYS3).
      real	clamp	! Sample clamp measure: 0 - no clamping, 
			!                       1 - heavy clamping
      real	cover	! Coverage value (in steradians)
      real	covlim  ! Lower coverage limit for valid data (steradians)
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      integer	offset	! Offset into internal file
      real	speed	! Factor to slow down rate of change of background

      parameter (speed=0.5)

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* SEE IF A NEW BACKGROUND IS REQUIRED. IF NOT, JUMP TO END.
*
      call gt2diw('BACKOUT',102,.true.,B0_nps,B0_nls,ival,istat)
      if(istat.ne.0) goto 999

      if(ZZ_ilv.ge.3) call wruser('  Creating new background...',istat)

*
* USE OPUS TO PUT SIMULATED DATA IN FILE 23
*
      call opus(1,23)

*
* CALCULATE THE ABSOLUTE DATA RESIDUALS, CONVERT THEM TO SURFACE 
* BRIGHTNESS RESIDUALS, AND STORE IN FILE 25. POSITIVE DATA RESIDUALS
* CANNOT BE CLAMPED BY THE BACKGROUND, SO SET THEM TO ZERO.
*
      do offset=1,ME_mk-1
         ME_st(ME_kb(25)+offset)=min(0.0, (ME_st(ME_kb(21)+offset)-
     :                                     ME_st(ME_kb(23)+offset))/
     :                                     ME_st(B6_sol+offset) )
      enddo

*
* GET THE SCALING FACTOR APPLIED BY MEMSYS3 TO THE ACCURACIES 
* STORED IN FILE 22
*
      if(ZZ_mth.eq.'HISTORIC') then
         c=1.0
      else
         c=1.0/ME_sig
      endif

*
* PRODUCE A MEASURE OF HOW MUCH EACH SAMPLE WAS "CLAMPED" BY THE 
* BACKGROUND. THIS IS DONE BY COMPARING THE SIMULATED DATA VALUES
* TO THE CORRESPONDING SAMPLE ERRORS. SIMULATED VALUES WHICH ARE
* LOWER THAN THEIR ERROR VALUE ARE CONSIDERED TO BE CLAMPED. 
* THE RESIDUALS ARE MULTIPLIED BY THESE CLAMP MEASURES.
*
      do offset=1,ME_mk-1
         clamp=max(0.0, 
     :            1.0-c*ME_st(ME_kb(23)+offset)*ME_st(ME_kb(22)+offset))
         ME_st(ME_kb(25)+offset)=ME_st(ME_kb(25)+offset)*clamp
      enddo


*
* USE TROPUS TO CONVERT THE RESIDUAL DATA INTO AN IMAGE IN FILE 20
*
      call tropus(25,20)

*
* DIVIDE THE IMAGE BY THE COVERAGE IMAGE TO GET CORRECT NORMALISATION
* AND ADD ON THE OLD BACKGROUND
*
      covlim=ZZ_psz*ZZ_psz*0.84616E-7
      do offset=0,ME_mj-1
         cover=ME_st(ME_kb(2)+offset)
         if(cover.gt.covlim) then
            ME_st(ME_kb(20)+offset)=speed*ME_st(ME_kb(20)+offset)/cover+
     :                              ME_st(B6_bac+offset)
         else
            ME_st(ME_kb(20)+offset)=PR_rin
         endif
      enddo

*
* OUTPUT THE NEW BACKGROUND TO DISK
*
      call memca2('BACKOUT',20,.true.,istat)

*
* FINISH
*
  999 continue

      end
