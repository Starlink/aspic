      subroutine ccorrn(crdin,smpin,crdout,nsamp,base,ndets,band,frmid,
     :                  filter,ilevel,work1,work2,work3,work4,work5,
     :                  device,mode,istat)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Generates output CRDD by adding a base line onto the input
*	CRDD, such that the output CRDD and sampled CRDD have the
*	same background.
*
*SOURCE
*       CCORRN.FOR in CRDDSAMPLE.TLB
*
*METHOD
*	For each sample in the input CRDD file, the difference between
*	the input sample value and the value of the sky image at the 
*	sample centre (as given in argument SMPIN) is found. Features
*	smaller than the value of argument FILTER are removed from
*	these differences (processing one data stream at a time). The
*	remaining differences are fitted using a least squares linear
*       fit ( if a flat base line is required then the gradient of
*	this linear fit is forced to be zero). Difference values
*	further than 3 sigma from the fitting line are rejected and
*	a new fitting line calculated. This is repeated 3 times to
* 	reject aberant points from the fit.
*	   The fit is sampled at the position of each data sample in
*	the input CRDD. The fit value is subtracted off the input
*	CRDD value to produce the output CRDD value.
*       
*ARGUMENTS       
*   INPUTS:
*	crdin(nsamp,ndets)   integer  The input CRDD.
*	smpin(nsamp,ndets)   integer  CRDD formed by sampling a sky
*				      image at the positions of the 
*				      samples in the input CRDD file.
*	nsamp		     integer  The no. of samples per detector
*	ndets		     integer  The no. of detector data streams
*	base		   character  The form of base line to be added
*				      to the input CRDD. 'LINEAR' or 
*				      'CONSTANT'.
*	band		     integer  The IRAS band no. of the input
*				      CRDD (1-4).
*	frmid		     integer  Pointer to the input CRDD 
*				      descriptor values.
*	filter		     integer  The size (in samples) of the box 
*				      filter to use when rejecting 
*				      sources from the data.
*	ilevel		     integer  Amount of info to display on the
*				      users terminal (1=none 3=max).
*	work1(nsamp,ndets)   real     Work space
*	work2(nsamp,ndets)   real     Work space
*	work3(nsamp,ndets)   integer  Work space
*	work4(nsamp)         real     Work space
*	work5(nsamp)         integer  Work space
*	device		   character  Graphics device name
*	mode		   character  CRDD type: AO or SURVEY
*   OUTPUTS:
*	crdout(nsamp,ndets)  integer  The output CRDD. This has a
*				      linear or constant base line added
*				      onto it to bring it into line with
*				      the sampled CRDD. 
*	istat		     integer  Error status; 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              ffrej,wruser
*	THIS PACKAGE (CRDDSAMPLE.TLB):
*	       pltdif
*	EDRS:
*	       wrerr
*STARLINK PARAMETERS
*	ALLREJ(error)	Accessed if FFREJ removes all the data
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 23/2/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE IRAS MISSION PARAMETERS, CRDD DESCRIPTOR VALUES AND DETECTOR
* DATA
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DS_COM)'
      include 'UTILITIES(AO_COM)'
      include 'UTILITIES(DT_DAT)'

*
* DECLARE ARGUMENTS
*
      integer	nsamp,ndets,band,frmid,istat,filter,ilevel
      integer	crdin(nsamp,ndets),smpin(nsamp,ndets),
     :          crdout(nsamp,ndets),work3(nsamp,ndets),work5(nsamp)
      character	base*(*),device*(*),mode*(*)
      real	work1(nsamp,ndets),work2(nsamp,ndets),work4(nsamp)

*
* DECLARE LOCAL VARIABLES
*
      real	c	! Offset of fitting line
      real	change	! Correction to an individual i/p CRDD sample
      real	cscale	! CRDD scale factor which gives scaled values
			! in Janskys
      real	csum	! Sum of changes
      real	czero	! CRDD zero offset which gives scaled values
			! in Janskys
      real	c2sum	! Sum of changes squared
      real	datval	! Scaled CRDD value
      real	den	! The denominator of the fit parameters
      integer	det	! Detector counter
      real	gamma	! No. of sigma to clip the fit at
      integer	in	! Unscaled value from the input CRDD
      integer	inval	! Flag for invalid unscaled CRDD samples
      integer	iter	! Iteration count for fitting process
      real	linval	! Value of fitting line at current sample
      real	m	! Gradient of fitting line
      real	mean	! Mean change introduced into CRDD
      real	mnnois	! Min value of NOISE over all detectors
      real	mxnois	! Max value of NOISE over all detectors
      integer	ncsum	! No. of changed samples
      real	newval	! Scaled, corrected input CRDD
      integer	ngood	! No. of valid sampels remaining after source rejection
      integer	niter	! No. of iterations to perform when fitting
      real	noise	! RMS noise between detector data and sampled data
      real	noisum	! Sum of NOISE values for all detectors
      integer	nnsum	! No. of contributions to NOISUM
      integer	nsum	! No. of difference values included in fit
      character	prbuf*80! Buffer for screen output
      real	rinval	! Flag for invalid scaled CRDD samples
      real	rms	! RMS change introduced into CRDD
      integer	samp	! Sample counter
      real	sigma	! RMS deviation of differences from fit
      integer	smp	! Unscaled value from the sampled CRDD
      real	xsum	! Sum of sample counters
      real	x2sum	! Sum of sample counters squared
      real	xysum	! Sum of product of sample and difference
      real	ysum	! Sum of difference values
      real	y2sum	! Sum of difference values squared

*
*  DEFINE PARAMETER VALUES
*
      parameter	(gamma =3.0,
     :           niter =4,
     :           rinval='FFFFFFFF'X)

*
* STORE COMMONLY USED VALUES
*
      if(mode.eq.'SURVEY') then
         inval=DS_bpx(frmid)
         cscale=DS_bsc(frmid)*DT_cvf(band)*DT_o84(band)
         czero=DS_bze(frmid)*DT_cvf(band)*DT_o84(band)
      else
         inval=AO_bpx(frmid)
         cscale=AO_bsc(frmid)*DT_cvf(band)*DT_o84(band)
         czero=AO_bze(frmid)*DT_cvf(band)*DT_o84(band)
      endif

*
* CALCULATE THE DIFFERENCES BETWEEN THE INPUT AND SAMPLED CRDD AND 
* TEMPORARILY STORE THEM IN THE OUTPUT CRDD ARRAY. SAMPLES WHICH ARE
* INVALID IN EITHER THE INPUT CRDD OR THE SAMPLED CRDD HAVE THEIR
* DIFFERENCE FLAGGED AS INVALID
*

      do det=1,ndets
         do samp=1,nsamp

            in=crdin(samp,det)
            smp=smpin(samp,det)

            if(in.ne.inval.and.smp.ne.inval) then
               crdout(samp,det)=in-smp
            else
               crdout(samp,det)=inval
            endif

         enddo
      enddo

*
* REMOVE BRIGHT FEATURES FROM THE DIFFERENCES (RESULTS STORED IN WORK1)
* FFREJ REPEATADLY SMOOTHS THE DATA WITH A BOX FILTER OF SIZE "FILTER"
* AND REJECTS INPUT SAMPLES FURTHER THAN 3 SIGMA FROM THE CORRESPONDING
* SMOOTHED VALUE
*
      call ffrej(crdout,nsamp,ndets,inval,6,3.0,filter,1,filter,1,
     :           ilevel,cscale,czero,sigma,0.1,work1,rinval,ngood,work2,
     :           work3,work4,work5)

*
* IF ALL THE DATA HAS BEEN REJECTED, QUIT
*
      if(ngood.eq.0) then
         call wrerr('ALLREJ')
         istat=1
         goto 999
      endif

*
* INITIALISE SUMS USED FOR MEASURING THE TOTAL EFFECT OF THE 
* CORRECTIONS AND NOISE STATISTICS
*
      csum=0.0
      c2sum=0.0
      ncsum=0

      noisum=0.0
      nnsum=0
      mxnois=0.0
      mnnois=1.0E32

*
* LOOP ROUND ALL THE DETECTORS, ADDING A BASE LINE ONTO EACH IN TURN
*
      do det=1,ndets

*
* ENTER THE FITTING LOOP. ON EACH ITERATION A LEAST SQUARES STRAIGHT
* LINE IS FITTED TO THOSE DIFFERENCES WHICH ARE CLOSER THAN 3 SIGMA TO 
* THE FIT USED IN THE LAST ITERATION.
*
         do iter=1,niter

*
* INITIALISE THE SUMS REQUIRED TO CALCULATE THE LEAST SQUARES STARIGHT 
* LINE PARAMETERS
*
            xsum=0.0
            x2sum=0.0
            xysum=0.0
            ysum=0.0
            y2sum=0.0
            nsum=0

*
* LOOP THOUGH ALL THE VALID DIFFERENCE VALUES (I.E. ONES NOT REJECTED
* BY FFREJ, OR CAUSED BY INVALID INPUT OR SAMPLED DATA)
*
            do samp=1,nsamp

               datval=work1(samp,det)
               if(datval.ne.rinval) then

*
* ON THE FIRST ITERATION ALL VALID DIFFERENCES ARE INCLUDED IN THE 
* FITTING
*
                  if(iter.eq.1) then

                     xsum=xsum+real(samp)
                     x2sum=x2sum+real(samp*samp)
                     xysum=xysum+datval*real(samp)
                     ysum=ysum+datval
                     y2sum=y2sum+datval*datval
                     nsum=nsum+1

*
* ON SUBSEQUENT ITERATIONS, ONLY THOSE DIFFERENCES WHICH LIE CLOSER
* THAN 3 SIGMA TO THE LAST FIT ARE USED
*
                  else

                     linval=m*real(samp)+c

                     if(abs(linval-datval).lt.gamma*sigma) then
                        xsum=xsum+real(samp)
                        x2sum=x2sum+real(samp*samp)
                        xysum=xysum+datval*real(samp)
                        ysum=ysum+datval
                        y2sum=y2sum+datval*datval
                        nsum=nsum+1
                     endif

                  endif

               endif

*
* DO THE NEXT DIFFERENCE
*
            enddo
   
*
* IF POSSIBLE, FORM THE PARAMETERS OF THE LEAST SQUARES FIT
*
            den=nsum*x2sum-xsum*xsum
            if(den.ne.0.0) then
   
               if(base.eq.'LINEAR') then
                  m=(nsum*xysum-xsum*ysum)/den
                  c=(x2sum*ysum-xsum*xysum)/den
               else
                  m=0.0
                  c=ysum/nsum
               endif         

*
* FIND THE RMS NOISE BETWEEN THE INPUT AND SAMPLED CRDD
*
               noise=sqrt(y2sum/nsum)

*
* FIND THE RMS DEVIATION OF THE DIFFERENCES FROM THE FIT
*   
               y2sum=0.0
               do samp=1,nsamp
                  datval=work1(samp,det)
                  if(datval.ne.rinval) then
                      y2sum=y2sum+(datval-(m*samp+c))**2
                  endif
               enddo

               sigma=sqrt(y2sum/nsum)


*
* IF THE FIT COULD NOT BE DETERMINED, SET ALL THE OUTPUT CRDD INVALID
* FOR THIS DETECTOR, THEN JUMP TO THE NEXT DETECTOR.
*   
            else
               do samp=1,nsamp
                  crdout(samp,det)=inval
               enddo
               goto 20
            endif

*
* DO NEXT ITERATION
*
         enddo

*
* SUBTRACT THE FITTED DIFFERENCE FROM THE INPUT DATA
*
         do samp=1,nsamp

            in=crdin(samp,det)
            if(in.ne.inval) then

               change=m*samp+c
               csum=csum+change
               c2sum=c2sum+change*change
               ncsum=ncsum+1

               newval=in*cscale+czero-change
               crdout(samp,det)=nint((newval-czero)/cscale)
            else
               crdout(samp,det)=inval
            endif

         enddo

*
* IF REQUIRED PRODUCE PLOT OF THE DIFFERENCE DATA AND BASE LINE FIT
*
         if(device.ne.'NONE') then
            call pltdif(work1(1,det),nsamp,rinval,m,c,work2(1,1),
     :                  work2(1,3),DT_bal(det,band))
         endif

*
* INCREMENT THE NOISE STATISTICS
*
         noisum=noisum+noise
         nnsum=nnsum+1
         mxnois=max(mxnois,noise)
         mnnois=min(mnnois,noise)

*
* DO NEXT DETECTOR
*
  20     continue
      enddo
         
*
* DISPLAY STATISTICS OF CHANGE
*
      if(ilevel.gt.1) then
         if(ncsum.gt.0) then

            call wruser(' ',istat)

            rms=sqrt(c2sum/ncsum)
            write(prbuf,30) rms
   30       format('  RMS change : ',G13.6,' Jy')
            call wruser(prbuf,istat)

            mean=csum/ncsum
            write(prbuf,40) mean
   40       format('  Mean change: ',G13.6,' Jy')
            call wruser(prbuf,istat)

            if(nnsum.gt.0) then

               write(prbuf,50) noisum/nnsum
   50          format('  Mean detector noise: ',G13.6, ' Jy')
               call wruser(prbuf,istat)

               write(prbuf,60) mxnois
   60          format('  Max. detector noise: ',G13.6, ' Jy')
               call wruser(prbuf,istat)

               write(prbuf,70) mnnois
   70          format('  Min. detector noise: ',G13.6, ' Jy')
               call wruser(prbuf,istat)

            else
               call wruser('*** Detector noise undefined',istat)
            endif

            call wruser(' ',istat)

         else
            call wruser('*** Output contains no valid data',istat)

         endif
      endif


*
* FINISH
*
  999 continue

      end
