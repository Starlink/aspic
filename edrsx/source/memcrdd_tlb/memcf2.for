      subroutine memcf2(sky,inserr,xscerr,ntrial,varn,simdat,skyfft,
     :                  prdfft,smooth,work,field,maxvar,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates the variance introduced into each usable data 
*	sample due to pointing errors.
*
*SOURCE
*       MEMCF2.FOR in MEMCRDD.TLB
*
*METHOD
*	The given sky image is used to produce simulated data. Each
*	sample is simulated many times with random pointing errors
*	introduced into each simulation. The variance of the simulated
*	data samples is used as an estimate of the data sample variance.
*	The pointing errors are gaussian with different FWHM in the 
*	cross scan and in scan directions.
*       
*ARGUMENTS       
*   INPUT:
*	sky	real	Image holding input sky
*	ntrial	integer	No. of trial simulations to perform on each 
*			data sample
*	xscerr	real	Cross scan pointing error in arcmins
*	inserr	real	In scan pointing error in arcmins
*	skyfft  real	Image work space to hold FFT of sky
*	prdfft  real	Image work space to hold FFT of smoothed sky
*	smooth	real	Image work space to hold smoothed sky
*	work	real	Image work space used in FFT routines
*	field	real	Data set holding field variances
*   OUTPUTS:
*       varn	real	Output variances
*	simdat	real	Data simulated from given sky image
*	maxvar	real	Maximum output variance
*
*COMMON USAGE
*   READ:
*	/A8_COM/,/B0_COM/,/B5_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcc6,memcc7,memcd0
*	NAG:
*	       g05cce,g05dde
*
*VAX SPECIFICS
*       implicit none
*	trig functions in degrees
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

* ... NUMBER OF SAMPLE GROUPS
      include '(A8_COM)'

* ... OUTPUT IMAGE FITS DESCRIPTORS
      include '(B0_COM)'

* ... START AND END OF EACH GROUP
      include '(B5_COM)'

* ... POINTERS TO MEMCRDD FILES STORED IN ME_ST
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      real	sky(B0_nps,B0_nls),varn(ME_mk),skyfft(B0_nls,B0_nps),
     :          smooth(B0_nps,B0_nls),prdfft(B0_nls,B0_nps),
     :          work(B0_nps,B0_nls),field(ME_mk),simdat(ME_mk)
      integer	ntrial,ierr
      real	xscerr,inserr,maxvar

*
* DECLARE LOCAL VARIABLES
*
      real	c1	! Contribution from bottom left pixel
      real	c2	! Contribution from bottom right pixel
      real	c3	! Contribution from top left pixel
      real	c4	! Contribution from top right pixel
      real	cosfac  ! Cosine factor to rotate PSF into sky frame
      real	datcen	! Sample value at the nominal sample position
      real	datval	! Data value in Jy/st
      real	delpix  ! Pixel increment due to pointing error
      real	dellin  ! Line increment due to pointing error
      real	dx      ! Right hand pixels interpolation weight
      real	dy	! Top pixels interpolation weight
      real	fldsig	! Sigma due to field noise only
      real	g05dde  ! NAG function giving sample from a Gaussian PDF
      integer	group	! Current sample group number
      real	inlim	! Limit to in scan deviation
      real	inscan  ! In-scan error for this simulation in arcmins
      integer	istat	! Temporary status value
      integer	line	! Line no. within sky image
      real	mean	! Mean of squared residuals (=variance of data)
      logical	more	! True if variance not sufficiently accurate yet
      integer	ngood   ! No. of samples which still lie within the sky 
			! frame after the position error has been added
      integer	pixel	! Pixel no. within sky image
      real	res	! Squared data residual
      integer	sample	! Sample index within data set
      real	sinfac  ! Sine factor to rotate PSF into sky frame
      integer	sgroup  ! Scan group of the current sample group
      real	solsq	! Square of samples solid angle
      real	stderr	! Standard error on mean of squared residuals
      real	sum	! Sum of squared residuals
      real	sumsq	! Sum of squared residuals, squared
      integer	trial	! Trial counter
      real	x	! Modified sample pixel position
      real	x0	! Original sample pixel position
      real	xlim	! Limit to cross scan deviation
      real	xscan	! Cross-scan error for this simulation in arcmins
      real	y	! Modified sample line position
      real	y0	! Original sample line position

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* INITIALISE NAG RANDOM NUMBER SEED TO A NON-REPEATABLE VALUE
*
      call g05cce

*
* SET THE LIMITS OF THE ALLOWED DISPLACEMENTS TO THREE SIGMA
*
      inlim=3.0*inserr
      xlim=3.0*xscerr

*
* TAKE FORWARD FFT OF INPUT SKY IMAGE
*
      call memcc6(PR_fwd,sky,skyfft,work,B0_nps,B0_nls,ierr)
      if(ierr.ne.0) goto 999

*
* LOOP ROUND EACH DETECTOR GROUP
*
      maxvar=0.0

      do group=1,A8_ngp

*
* IF THIS GROUP IS EMPTY. PASS ON
*
         if(B5_lgs(group).eq.0) goto 20

*
* MULTIPLY THE SKY TRANSFORM BY THE PSF TRANSFORM, 
*
         call memcd0(skyfft,ME_st(B6_psf(group)),prdfft,B0_nps,B0_nls)

*
* TAKE INVERSE FFT TO GET SMOOTHED SKY IMAGE
*
         call memcc7(PR_inv,prdfft,smooth,work,B0_nps,B0_nls,ierr)
         if(ierr.ne.0) goto 999

*
* CALCULATE SCAN GROUP NUMBER FOR THIS SAMPLE GROUP
*
         sgroup=mod(group-1,A8_nsg)+1

*
* SAVE SCAN ANGLE FUNCTIONS
*
         sinfac=sind(B0_fit(7)-A8_ang(sgroup))/ZZ_psz 
         cosfac=cosd(B0_fit(7)-A8_ang(sgroup))/ZZ_psz

*
* FIND THE VARIANCE FOR EVERY SAMPLE IN THE CURRENT GROUP
*
         do sample=B5_fgs(group),B5_lgs(group)

*
* CALCULATE THE DATA VALUE AT THE SAMPLE CENTRE (I.E. WITH NO POINTING
* ERROR), AND STORE IN THE OUTPUT SIMDAT ARRAY
*
            call memcc9(sample,c1,c2,c3,c4,pixel,line)
            datcen = c1*smooth(pixel,line)
     :              +c2*smooth(pixel+1,line)
     :              +c3*smooth(pixel,line+1)
     :              +c4*smooth(pixel+1,line+1)
            simdat(sample)=datcen*ME_st(B6_sol+sample-1)

*
* STORE THE COORDS OF THE CENTRE OF THIS DATA SAMPLE, ITS FIELD
* VARIANCE, AND THE SQUARE OF ITS SOLID ANGLE
*
            x0=ME_st(B6_x+sample-1)
            y0=ME_st(B6_y+sample-1)
            solsq=(ME_st(B6_sol+sample-1))**2
            fldsig=sqrt(field(sample)/solsq)

*
* INITIALISE SUM OF ABSOLUTE RESIDUALS BETWEEN OFF-CENTRE DATA VALUES 
* AND THE CENTRAL DATA VALUE. 
*
            sum=0.0
            sumsq=0.0
            ngood=0

*
* LOOP ROUND UNTIL POINTING VARIANCE IS FOUND TO SUFFICIENT ACCURACY
*
            more=.true.
            do while(more)

*
* DO A BATCH OF 20 TRIAL SAMPLES
*
               do trial=1,20

*
* CALCULATE RANDOM IN-SCAN AND CROSS-SCAN DISPLACEMENTS (IN ARCMINS)
* FROM GUASSIAN DISTRIBUTIONS. CLIPPED AT 3 SIGMA.
*
  10              inscan=g05dde(0.0,inserr)
                  if(abs(inscan).gt.inlim) goto 10

  11              xscan=g05dde(0.0,xscerr)
                  if(abs(xscan).gt.xlim) goto 11

*
* CONVERT THESE INTO PIXEL AND LINE DISPLACEMENTS IN THE IMAGE FRAME
*
                  delpix=inscan*sinfac-xscan*cosfac
                  dellin=inscan*cosfac+xscan*sinfac

*
* CALCULATE THE CONTRIBUTIONS TO THE BI-LINEARLY INTERPOLATED VALUE 
* FROM THE FOUR SKY PIXELS NEAREST TO THE THE DATA SAMPLE CENTRE
*
                  x=x0+delpix
                  pixel=int(x)
                  if(pixel.gt.0) then
                     if(pixel.lt.B0_nps) then
   
                        y=y0+dellin
                        line=int(y)
                        if(line.gt.0) then
                           if(line.lt.B0_nls) then
   
                              dx=x-pixel
                              dy=y-line
                              c1=(1.0-dx)*(1.0-dy)
                              c2=dx*(1.0-dy)
                              c3=(1.0-dx)*dy
                              c4=dx*dy

*
* CALCULATE THE INTERPOLATED VALUE OF THE SMOOTHED IMAGE AND INCREMENT
* THE TRIAL SUMS
*
                              datval = c1*smooth(pixel,line)
     :                                +c2*smooth(pixel+1,line)
     :                                +c3*smooth(pixel,line+1)
     :                                +c4*smooth(pixel+1,line+1)

                              res=abs(datval-datcen)
                              sum=sum+res
                              sumsq=sumsq+res*res
                              ngood=ngood+1

                           endif
                        endif
                     endif
                  endif

*
* DO NEXT TRIAL IN THIS BATCH
*
               enddo

*
* CALCULATE THE MEAN ABSOLUTE RESIDUAL AND THE STANDARD ERROR OF THE 
* MEAN.
*
               if(ngood.gt.0) then
                  mean=sum/ngood
                  stderr=sqrt(max(0.0,((sumsq/ngood)-(mean**2))/ngood))

*
* IF THE ERROR IN THE MEAN IS LESS THAN THE FIELD NOISE, OR IS LESS
* THAN 5% OF THE TOTAL MEAN, THEN SUFFICIENT ACCURACY HAS BEEN 
* REACHED
*
                  if(stderr.lt.fldsig.or.stderr.lt.0.02*mean) 
     :                                                   more=.false.
               endif

*
* IF THE MAXIMUM NO. OF TRIALS HAS BEEN REACHED THEN USE THE MOST
* RECENT VARIANCE VALUE
*
               if(ngood.ge.ntrial) more=.false.

*
* DO NEXT BATCH
*
            enddo

*
* WHEN THE MEAN IS DECIDED ON, CONVERT IT TO A VARIANCE IN (JY)**2
*
            if(ngood.gt.0) then
               varn(sample)=mean*mean*solsq
               maxvar=max(maxvar,varn(sample))
            else
               varn(sample)=1.0E20
            endif

*
* GO ROUND FOR NEXT DATA SAMPLE
*
         enddo

*
* DO NEXT DETECTOR GROUP
*
  20     continue

      enddo

*
* FINISH
*
  999 continue

      end
