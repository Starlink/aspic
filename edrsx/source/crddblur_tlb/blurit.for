      subroutine blurit(crdd,res,nsamp,ndet,inval,scale,zero,ratea,
     :                  rateb)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Apply an in-scan smoothing filter to a CRDD data set. The 
* 	filter is the in-scan PSF of any IRAS band.
*
*SOURCE
*       BLURIT.FOR in CRDDBLUR.TLB
*
*METHOD
*	The PSFs for all 4 IRAS bands are assumed to be of the same
*       shape, but scaled in the in-scan direction by a factor equal to
*	the ratio of the bands sampling frequencies. This can be seen 
* 	to be a reasonable approximation from the in-scan band-averaged
*	PSFs displayed in the Explanatory Supplement .
*	    Rather than storing a single PSF and then scaling and 
*	sampling it at the positions of the given CRDD samples, 5 copies
*	of the PSF are defined in data statements in pre-scaled and 
*	sampled form. The filter to use is selected on the basis of the
*	ratio of the samplingh frequencies of the input CRDD band and 
*	blurring band.
*          The blurred output CRDD is stored with the same scale and 
*	zero factors as the input CRDD. Should any sample value overflow
*	the output scaling (which should never happen) it is set blank.
*
*ARGUMENTS       
*   INPUTS:
*	crdd(nsamp,ndet)  integer	The input unscaled CRDD
*	nsamp		  integer	No. of samples per detector
*	ndet		  integer	No. of detectors in the CRDD
*	inval		  integer	Flag for blank samples
*       scale	          real		Input scaling factor
*	zero		  real		Input zero point offset
*	ratea		  integer	Sampling frequency of the input
*					CRDD band.
*	rateb		  integer	Sampling frequency of the 
*					blurring band.
*   OUTPUTS:
*	res(nsamp,ndet)	  integer	Output smoothed CRDD in unscaled
*					integer format
*
*SUBROUTINES CALLED
*       none
*              
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 21/3/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer nsamp,ndet,inval,crdd(nsamp,ndet),res(nsamp,ndet),ratea,
     :        rateb
      real    scale,zero
      
*
* DECLARE LOCAL VARIABLES
*

      integer det	      ! Current detector (cross scan order)
      real    filter(-20:20,5)! Array holding filters with which
                              ! data will be smoothed
      integer fsamp	      ! Sample to be multiplied by the filter
      integer fsize	      ! Half size of smoothing filter in samples
      integer irat	      ! Pointer to smoothing filter to use
      real    ratio	      ! Ratio of input sample size to smoothing 
			      ! sample size
      real    result	      ! Temporary storage for integer results
      integer samp	      ! Current sample number
      real    sum	      ! Sum of sample values in weighted average
      real    wsum	      ! Sum of weights in weighted average

*
* SET UP THE ARRAY HOLDING SMOOTHING FILTERS FOR DIFFERENT RATIO
* OF SAMPLE SIZES. (POSSIBLE RATIO VALUES ARE 1/4, 1/2, 1, 2, 4)
*
      data filter/
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,1.000,
     :0.040,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
 
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.320,1.000,
     :0.360,0.040,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
  
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.060,0.320,0.790,1.000,
     :0.820,0.360,0.120,0.040,0.010,0.000,0.000,0.000,0.000,0.000,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
  
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     :0.001,0.000,0.014,0.060,0.155,0.320,0.556,0.790,0.943,1.000,
     :0.960,0.820,0.591,0.360,0.207,0.120,0.069,0.040,0.024,0.010,
     :0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
  
     :0.000,0.002,0.001,0.000,0.000,0.004,0.014,0.032,0.060,0.100,0.155,
     :0.228,0.320,0.433,0.556,0.679,0.790,0.878,0.943,0.983,1.000,
     :0.993,0.960,0.903,0.820,0.712,0.591,0.469,0.360,0.273,0.207,
     :0.157,0.120,0.091,0.069,0.052,0.040,0.031,0.024,0.017,0.010/

*
* CALCULATE THE RATIO OF SAMPLE SIZES BASED ON THE SAMPLING FREQUENCIES
*
      ratio=real(ratea)/real(rateb)

*
* CALCULATE INDEX INTO FILTER ARRAY WHICH HOLDS THE SMOOTHING FILTER
* APPROPRIATE FOR THIS RATIO VALUE
*
      irat=int(3.0+log(ratio)/log(2.0))

*
* CALCULATE HALF SIZE OF USEFUL PART OF SMOOTHING FILTER
*
      fsize=int(5.0*ratio)

*
* SMOOTH EACH DETECTOR STREAM IN TURN
*
      do det=1,ndet

*
* EACH SAMPLE IS REPLACED BY THE WEIGHTED MEAN OF ADJACENT SAMPLE VALUES
* WHERE THE WEIGHTS ARE GIVEN IN THE SMOOTHING FILTER
*
         do samp=1,nsamp

*
* INVALID PIXELS IN INPUT CRDD ARE LEFT INVALID
*
            if(crdd(samp,det).ne.inval) then

*
* FORM WEIGHTED MEAN OF ALL VALID SAMPLES WITHIN THE USEFUL WIDTH OF THE
* SMOOTHING FILTER
*
               sum=0
               wsum=0

               do fsamp=max(1,samp-fsize),min(nsamp,samp+fsize)
                  if(crdd(fsamp,det).ne.inval) then
                     sum=sum+(scale*crdd(fsamp,det)+zero)
     :                       *filter(fsamp-samp,irat)
                     wsum=wsum+filter(fsamp-samp,irat)
                  endif
               enddo

*
* IF ANY VALID CONTRIBUTION TO THE MEAN WAS FOUND, CALCULATE THE
* WEIGHTED MEAN. CONVERT THE MEAN TO ITS EQUIVALENT SCALED INTEGER
* VALUE FOR DISK STORAGE, GUARDING AGAINST OVERFLOW. IF SCALE FACTOR IS
* ZERO ALL RESULTS WILL BE ZERO IRRESPECTIVE OF THE INTEGER VALUE, SO
* JUST STORE 1 IF SCALE IS ZERO.
*
               if(wsum.gt.0) then
                  if(scale.ne.0.0) then
                     result=(sum/wsum-zero)/scale
                     if(abs(result).lt.2.0E9) then
                        res(samp,det)=nint(result)
                     else
                        res(samp,det)=inval
                     endif
                  else
                     res(samp,det)=1.0
                  endif
               else
                  res(samp,det)=inval
               endif

*
* ANY SAMPLES INVALID IN THE INPUT WILL ALSO BE INVALID IN THE OUTPUT
*
            else
               res(samp,det)=inval
            endif

*
* DO NEXT SAMPLE
*
         enddo

*
* DO NEXT DETECTOR DATA STREAM
*
      enddo
      
*
* FINISH
*
  999 continue

      end
