      subroutine memcd3(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Subtracts background data from the data stored in file 21.
*	The background image is stored in common for later use.
*
*SOURCE
*       MEMCD3.FOR in MEMCRDD.TLB
*
*METHOD
*	The background is defined by an image, NOT a data set. The
*	user supplies this image (possibly based on the results of a
*	previous run of MEMCRDD). Any invalid pixels are replace by
*       the mean of the valid pixels. If the user wishes it, a flat 
*	image can be used instead with a value such that the minimum 
*	data value is just negative (by one sigma). Either way, the 
*	image is used to create simulated data, and this data is 
*	subtracted from the data currently stored in file 21. The 
*	background image is stored in common for later use (pointed 
*	to by "B6_bac"). Any pixels which have very low data coverage
*	are set invalid in the stored background image.
*
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/ME_COM/ on entry file 21 should have the input data in Jy and
*                file 22 should contain the field variances.
*       /B6_COM,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcd1,memcf3,memce5
*       EDRS:
*              wrerr
*       INTERIM:
*	       frdata,cnpar
*
*STARLINK PARAMETERS
*	BACKGRND	An EDRS image containing the background
*	NOVALID(error)  Accessed if the input image contains no valid 
*			data
*
*VAX SPECIFICS
*       implicit none
*       %val
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
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

      include '(B5_COM)'

* ... POINTERS TO START OF INTERNAL FILES IN COMMON
      include '(B6_COM)'

* ... MEMSYS3 INFO
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
      real	backsb	! Default background surface brightness
      character	bdfnam*30! Name of BDF containing background image
      real	covlim	! Lower coverage limit for valid pixels
      real	flux	! Flux value from file 21 (in Jy)
      integer	istat	! Temporary status value
      real	maxsb	! Max corrected surface brightness
      real	minsb	! Min corrected surface brightness
      integer	nval	! No. of valid pixels in background image
      integer	offset	! Offset into an internal data set or image
      real	omega	! Sample solid angle in Steradians
      character	prbuf*80! Buffer for screen output
      real	sb	! Surface brightness value in Jy/St
      real	sigma	! The standard deviation of the field noise
      real	sum	! Sum of valid data in the background image
      real	sumsb	! Sum of the corrected surface brightnesses

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF REQUIRED TELL THE USER WHAT IS HAPPENING
*
      if(ZZ_ilv.ge.3) then
         call wruser(' ',istat)
         call wruser('  Subtracting background...',istat)
      endif

*
* ATTEMPT TO GET A BACKGROUND IMAGE FROM THE USER. 
*
      call memcf3('BACKGRND',.true.,ME_st(B6_bac),PR_rin,bdfnam,istat)

*
* IF NO IMAGE WAS OBTAINED, USE THE DEFAULT FLAT IMAGE
*
      if(istat.ne.0) then

*
* FIND A DEFAULT BACKGROUND VALUE SUCH THAT THE MINIMUM DATA VALUE
* AFTER BACKGROUND SUBTRACTION WOULD BE A GIVEN FRACTION (PR_RAT) OF
* THE MAXIMUM VALUE.  (PR_RAT=0.005 AT THE MOMENT 16/2/90)
*
         backsb=(B5_min-PR_rat*B5_max)/(1.0-PR_rat)

*
* IF REQUIRED, TELL THE USER WHAT THE BACKGROUND VALUE IS
*
         if(ZZ_ilv.ge.3) then
            write(prbuf,10) backsb
  10        format('  Default background surface brightness used: ',
     :             G13.6,' Jy/st')
            call wruser(prbuf,istat)
         endif

*
* SUBTRACT THIS SURFACE BRIGHTNESS OFF EACH DATA SAMPLE
*
         sumsb=0.0
         maxsb=-1.0E32
         minsb=1.0E32

         do offset=0,ME_mk-1
            flux=ME_st(ME_kb(21)+offset)
            omega=ME_st(B6_sol+offset)
            flux=flux-omega*backsb
            ME_st(ME_kb(21)+offset)=flux
            sb=flux/omega
            sumsb=sumsb+sb
            maxsb=max(maxsb,sb)
            minsb=min(minsb,sb)
         enddo

*
* FILL THE BACKGROUND IMAGE WITH THIS VALUE
*
         do offset=0,ME_mj-1
            ME_st(B6_bac+offset)=backsb
         enddo

*
* IF AN IMAGE WAS OBTAINED SUCCESSFULLY FROM THE USER...
*
      else

*
* FIND MEAN OF THE VALID PIXELS
*
         sum=0.0
         nval=0
         do offset=0,ME_mj-1
            sb=ME_st(B6_bac+offset)
            if(sb.ne.PR_rin) then
               sum=sum+sb
               nval=nval+1
            endif
         enddo

         if(nval.gt.0) then
            backsb=sum/nval

*
* IF IMAGE CONTAINED NO VALID PIXELS, ABORT.
*
         else
            call wrerr('NOVALID')
            ierr=1
            goto 999
         endif

*
* REPLACE ANY INVALID PIXELS IN THE IMAGE WITH THE MEAN VALUE
*
         if(nval.lt.ME_mj) then
            do offset=0,ME_mj-1
               if(ME_st(B6_bac+offset).eq.PR_rin) then
                  ME_st(B6_bac+offset)=backsb
               endif
            enddo
         endif            

*
* PRODUCE SIMULATED DATA FROM THIS IMAGE (MEMCD1 IS THE ROUTINE
* CALLED BY "OPUS"). STORE THE DATA IN FILE 23.
*
         call memcd1(ME_st(B6_bac),ME_st(ME_kb(23)),ME_st(B6_wk1),
     :               ME_st(B6_wk2),ME_st(B6_wk3),ME_st(B6_wk4))

*
* SUBTRACT THE BACKGROUND DATA FROM THE INPUT DATA
*
         sumsb=0.0
         maxsb=-1.0E32
         minsb=1.0E32

         do offset=0,ME_mk-1
            flux=ME_st(ME_kb(21)+offset)-ME_st(ME_kb(23)+offset)
            sb=flux/ME_st(B6_sol+offset)
            ME_st(ME_kb(21)+offset)=flux
            sumsb=sumsb+sb
            maxsb=max(maxsb,sb)
            minsb=min(minsb,sb)
         enddo

      endif

*
* IF REQUIRED TELL THE USER THE MAX MIN AND MEAN SURFACE BRIGHTNESS
* AFTER SUBTRACTION OF THE BACKGROUND
*
      if(ZZ_ilv.ge.3) then
         call wruser('  Data statistics after background subtraction'//
     :               ' (in Jy/St) ...',istat)

         write(prbuf,20) maxsb
  20     format('     Maximum surface brightness: ',G13.6)
         call wruser(prbuf,istat)      

         write(prbuf,30) sumsb/ME_mk
  30     format('     Mean surface brightness   : ',G13.6)
         call wruser(prbuf,istat)      

         write(prbuf,40) minsb
  40     format('     Minimum surface brightness: ',G13.6)
         call wruser(prbuf,istat)      

      endif

*
*  CREATE THE COVERAGE IMAGE IN FILE 2
*
      call memce5(ME_st(ME_kb(2)),.true.,ierr)

*
* SET ALL BACKGROUND PIXEL VALUES TO "INVALID" WHICH ARE LESS THAN THE
* LOWER COVERAGE LIMIT. THE SOLID ANGLE OF AN IMAGE PIXEL IS TAKEN
* AS THE COVERAGE LIMIT
*
      covlim=ZZ_psz*ZZ_psz*0.84616E-7
      do offset=0,ME_mj-1
         if(ME_st(ME_kb(2)+offset).lt.covlim) then
            ME_st(B6_bac+offset)=PR_rin
         endif
      enddo

*
* FINISH
*
  999 call frdata('BACKGRND',istat)
      call cnpar('BACKGRND',istat)

      end
