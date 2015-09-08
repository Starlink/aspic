      subroutine memce9(varn,simdat,work1,work2,work3,error,k,maxsol,
     :                  maxpsf,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Uses the image currently stored in file 1 to calculate the 
*	contribution to the variance of each residual caused by errors 
*	in the PSFs.
*
*SOURCE
*       MEMCE9.FOR in MEMCRDD.TLB
*
*METHOD
*	It is assumed that for each sample, the total variance due to 
*	PSF errors is the sum of two terms:
*	a) Due to the error in the values used for the effective solid 
*	   angle of each detector. It is assumed that the standard 
*	   deviation of a solid angle estimate is given by a constant
*	   ("k") times the solid angle estimate.
*	b) Due to pixel-to-pixel errors in the PSF image. This is itself
*	   made up of two parts:
*	   i) The error in each detector PSF. It is assumed that the
*	      standard deviation of any IPAC PSF value is given by a
*	      constant ("error") times the PSF value.
*	   ii)The error caused by using the mean of several detector
*	      PSFs instead of the single detector PSF.
*
*ARGUMENTS       
*   INPUT:
*	simdat(ME_mk)	     real     Data simulated from the given image
*	work1(B0_nps,B0_nls) real     Work space
*	work2(B0_nps,B0_nls) real     Work space
*	work3(B0_nps,B0_nls) real     Work space
*	k		     real     Relative error in solid angle values
*	error		     real     Intrinsic relative error in each
*				      PSF
*	ierr		     integer  Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	varn(ME_mk)	     real     The output variances
*	maxsol		     real     The maximum variance due to errors
*				      in the detector solid angles
*	maxpsf		     real     The maximum variance due to errors
*				      in the detector PSFs
*       ierr                 integer  Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A8_COM/,/B0_COM/,/B5_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcc6,memcc7,memcc9,memcd0,memcf0,opus
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/1/90
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

* ... NO. OF GROUPS
      include '(A8_COM)'

* ... FITS DESCRIPTORS OF HI-RES IMAGE
      include '(B0_COM)'

* ... GROUP BOUNDARIES
      include '(B5_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFO
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr
      real	work1(B0_nps,B0_nls),work2(B0_nps,B0_nls),simdat(ME_mk),
     :          work3(B0_nps,B0_nls),error,k,maxsol,maxpsf,varn(ME_mk)

*
* DECLARE LOCAL VARIABLES
*
      real	c1	! Contribution from bottom left pixel
      real	c2	! Contribution from bottom right pixel
      real	c3	! Contribution from top left pixel
      real	c4	! Contribution from top right pixel
      real	datvl0	! Sum of variances of all PSF pixels
      real	datvl1	! Smoothed "first order" variance
      real	datvl2	! Smoothed "second order" variance
      integer	group	! Current sample group number
      integer	istat	! Local error status
      integer	line	! Line no. within sky image
      real	mean	! Mean value of squared hi-res image
      integer	offset	! Offset into a data set or image
      real	omega	! Sample solid angle
      integer	pixel	! Pixel no. within sky image
      real	psfvar	! Variance due to uncertainty in PSFs
      integer	sample	! Sample index within data set
      real	simval	! Simulated data value
      real	sum	! Sum of real values
      real	value	! Temporary storage for a real value

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* THE VARIANCES IN THE QUOTED DETECTOR SOLID ANGLES ARE ASSUMED TO BE
* GIVEN BY A CONSTANT TIMES THE SOLID ANGLE SQUARED. THESE CAUSE
* ERRORS IN THE SIMULATED DATA GIVEN BY THE CONSTANT TIMES THE
* SIMULATED DATA VALUES. STORE THESE CONTRIBUTIONS TO THE VARIANCE 
* THE OUTPUT.
*
      maxsol=0.0

      do sample=1,ME_mk
         varn(sample)=(simdat(sample)*k)**2
         maxsol=max(maxsol,varn(sample))
      enddo
      
*
* OVERWRITE INTERNAL FILE 2 WITH THE SQUARE OF THE IMAGE 
* CURRENTLY HELD IN FILE 1. 
*
      maxpsf=0.0

      do offset=0,ME_mj-1
         ME_st(ME_kb(2)+offset)=ME_st(ME_kb(1)+offset)**2
      enddo

*
* OVERWRITE IMAGES 1 AND 2 WITH THEIR FFTS
*
      call memcc6(PR_fwd,ME_st(ME_kb(1)),ME_st(ME_kb(1)),work1,B0_nps,
     :            B0_nls,ierr)

      call memcc6(PR_fwd,ME_st(ME_kb(2)),ME_st(ME_kb(2)),work1,B0_nps,
     :            B0_nls,ierr)

*
* LOOP ROUND EACH USED DETECTOR SAMPLE GROUP
*
      do group=1,A8_ngp
         if(B5_lgs(group).gt.0) then         

*
* CALCULATE AN IMAGE HOLDING THE VARIANCE OF THE PSF VALUE AT EACH PIXEL
* (IT IS WRITTEN INTO ARRAY WORK1).
*
            call memcf0(error,group,work1,work2,work3,datvl0,ierr)

*
* OVERWRITE PSF VARIANCE IMAGE WITH ITS OWN FFT
*
            call memcc6(PR_fwd,work1,work1,work2,B0_nps,B0_nls,ierr)
            if(ierr.ne.0) goto 999

*
* MULTIPLY FFT OF PSF VARIANCES BY FFT OF THE SKY IMAGE, STORING THE 
* RESULT IN ARRAY WORK2
*
            call memcd0(ME_st(ME_kb(1)),work1,work2,B0_nps,B0_nls)

*
* TAKE INVERSE FFT TO GET SMOOTHED "FIRST ORDER" VARIANCES IN WORK2
*
            call memcc7(PR_inv,work2,work2,work3,B0_nps,B0_nls,ierr)
            if(ierr.ne.0) goto 999

*
* MULTIPLY FFT OF PSF VARIANCES BY FFT OF THE SQUARED SKY IMAGE, 
* STORING THE RESULT IN ARRAY WORK3
*
            call memcd0(ME_st(ME_kb(2)),work1,work3,B0_nps,B0_nls)

*
* TAKE INVERSE FFT TO GET SMOOTHED "SECOND ORDER" VARIANCES IN WORK3
*
            call memcc7(PR_inv,work3,work3,work1,B0_nps,B0_nls,ierr)
            if(ierr.ne.0) goto 999

*
* SAMPLE THE SMOOTHED VARIANCES AT THE POSITION OF EACH SAMPLE
*
            do sample=B5_fgs(group),B5_lgs(group)

*
* CALCULATE THE CONTRIBUTIONS TO THE BI-LINEARLY INTERPOLATED VALUE 
* FROM THE FOUR PIXELS NEAREST TO THE THE DATA SAMPLE CENTRE
*
               call memcc9(sample,c1,c2,c3,c4,pixel,line)

*
* CALCULATE THE INTERPOLATED VALUE OF THE SMOOTHED IMAGES
*
               datvl1 = c1*work2(pixel,line)
     :                 +c2*work2(pixel+1,line)
     :                 +c3*work2(pixel,line+1)
     :                 +c4*work2(pixel+1,line+1)
               datvl2 = c1*work3(pixel,line)
     :                 +c2*work3(pixel+1,line)
     :                 +c3*work3(pixel,line+1)
     :                 +c4*work3(pixel+1,line+1)

*
* FORM THE FINAL VARIANCE
*

               omega=ME_st(B6_sol+sample-1)
               simval=simdat(sample)

               psfvar=simval*simval*datvl0
     :                -2.0*simval*omega*datvl1
     :                +omega*omega*datvl2

               varn(sample)=varn(sample)+psfvar

               maxpsf=max(maxpsf,psfvar)

*
* GO ROUND FOR NEXT DATA SAMPLE
*
            enddo

*
* DO NEXT SAMPLE GROUP
*
         endif
      enddo

*
* FINISH
*
  999 continue

      end
